// importScripts('https://www.gstatic.com/firebasejs/8.4.1/firebase-app.js');
// importScripts('https://www.gstatic.com/firebasejs/8.4.1/firebase-messaging.js');

//    /*Update with yours config*/
//   const firebaseConfig = {
//    apiKey: "YOur Key-EW_U6NZU-A",
//    authDomain: "flutter-dasd-tes-fec9d.firebaseapp.com",
//    projectId: "flutter-notification-tes-fec9d",
//    storageBucket: "flutter-dsd-tes-fec9d.dasda.com",
//    messagingSenderId: "263307381024",
//    appId: "1:263307381024:web:dsds",
//    measurementId: "G-dsada"
//  };
//   firebase.initializeApp(firebaseConfig);
//   const messaging = firebase.messaging();

//   /*messaging.onMessage((payload) => {
//   console.log('Message received. ', payload);*/
//   messaging.onBackgroundMessage(function(payload) {
//     console.log('Received background message ', payload);

//     const notificationTitle = payload.notification.title;
//     const notificationOptions = {
//       body: payload.notification.body,
//     };

//     self.registration.showNotification(notificationTitle,
//       notificationOptions);
//   });
